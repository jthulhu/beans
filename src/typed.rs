use crate::{error::Result, parser::AST, span::Span};

macro_rules! node {
    ($ast:expr) => {
        if let $crate::parser::AST::Node {
            attributes, span, ..
        } = $ast
        {
            (attributes, span)
        } else {
            panic!("expected to find node, found {:?}", $ast);
        }
    };
}

macro_rules! get {
    ($node:expr => $key:literal) => {
        $node.0.remove($key).unwrap()
    };
    ($node:expr => $key:ident) => {
        $node.0.remove(stringify!($key)).unwrap()
    };
}

macro_rules! value {
    ($node:expr => $key:literal) => {
        if let $crate::parser::AST::Literal {
            value: $crate::parser::Value::Str(result),
            ..
        } = get!($node => $key)
        {
            result
        } else {
            panic!("expected to find value")
        }
    };
    ($node:expr => $key:ident) => {
        if let $crate::parser::AST::Literal {
            value: $crate::parser::Value::Str(result),
            ..
        } = get!($node => $key)
        {
            result
        } else {
            panic!("expected to find value")
        }
    };
}

macro_rules! spanned_value {
    ($node:expr => $key:literal) => {
        if let $crate::parser::AST::Literal {
            value: $crate::parser::Value::Str(result),
            span,
            ..
        } = get!($node => $key)
        {
            Spanned::new(result, span.unwrap())
        } else {
            panic!("expected to find value")
        }
    };
    ($node:expr => $key:ident) => {
        if let $crate::parser::AST::Literal {
            value: $crate::parser::Value::Str(result),
            span,
            ..
        } = get!($node => $key)
        {
            Spanned::new(result, span.unwrap())
        } else {
            panic!("expected to find value")
        }
    };
}

macro_rules! match_variant {
    (($node:expr) {
	$($variant:ident => $code:expr),* $(,)?
    }) => {{
	let variant = value!($node => variant);
	let result = match &*variant {
	    $(stringify!($variant) => $code,)*
		found_variant => panic!("Unexpected variant {found_variant}, expected {:?}", [$(stringify!($variant)),*]),
	};
	$crate::typed::Spanned {
	    span: $crate::typed::span!($node),
	    inner: result,
	}
    }};
}

macro_rules! span {
    ($node:expr) => {
        $node.1.clone()
    };
}

// Publicly export macros. This trick allows macros not to be at
// toplevel
pub(crate) use {get, match_variant, node, span, spanned_value, value};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T: ?Sized> {
    pub span: Span,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { span, inner }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }
}

impl<T> Spanned<Option<T>> {
    pub fn transpose(self) -> Option<Spanned<T>> {
        self.inner.map(|inner| Spanned {
            span: self.span,
            inner,
        })
    }
}

impl<T> Spanned<Spanned<T>> {
    pub fn merge(self) -> Spanned<T> {
        Spanned {
            span: self.span.sup(&self.inner.span),
            inner: self.inner.inner,
        }
    }
}

pub trait Tree: Sized {
    fn read(ast: AST) -> Result<Self>;
    fn span(&self) -> &Span;
}

impl<T: Tree> Tree for Spanned<Option<T>> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(match_variant! {(node) {
            None => None,
            Some => Some(T::read(get!(node => value))?),
        }})
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
struct NonemptyVec<T> {
    vec: Vec<T>,
    span: Span,
}

impl<T: Tree> Tree for NonemptyVec<T> {
    fn read(mut ast: AST) -> Result<Self> {
        let mut vec = Vec::new();
        let span = ast.span().unwrap().clone();
        loop {
            let mut node = node!(ast);
            match_variant! {(node) {
                Cons => {
                    vec.push(get!(node => head).to_tree()?);
                    ast = get!(node => tail);
                },
                Nil => {
                    vec.push(get!(node => head).to_tree()?);
                    break Ok(Self {
			vec,
			span,
		    });
                },
            }};
        }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl<T: Tree> Tree for Spanned<Vec<T>> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(get!(node => value)
            .to_tree::<Spanned<Option<NonemptyVec<_>>>>()?
            .transpose()
            .map(|x| x.map(|nev| nev.vec))
            .unwrap_or_else(|| Spanned::new(Vec::new(), span!(node))))
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Tree for Spanned<()> {
    fn read(ast: AST) -> Result<Self> {
        Ok(Spanned::new((), ast.span().unwrap().clone()))
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Tree for Spanned<bool> {
    fn read(ast: AST) -> Result<Self> {
        Ok(ast
            .to_tree::<Spanned<Option<Spanned<()>>>>()?
            .map(|o| o.is_some()))
    }

    fn span(&self) -> &Span {
        &self.span
    }
}
