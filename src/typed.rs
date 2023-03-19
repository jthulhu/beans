use crate::parser::AST;

macro_rules! node {
    ($ast:expr) => {
        if let $crate::parser::AST::Node { attributes, .. } = $ast {
            attributes
        } else {
            panic!("expected to find node");
        }
    };
}

macro_rules! get {
    ($node:expr => $key:literal) => {
        $node.remove($key).unwrap()
    };
    ($node:expr => $key:ident) => {
        $node.remove(stringify!($key)).unwrap()
    };
}

macro_rules! value {
    ($node:expr => $key:literal) => {
	if let $crate::parser::AST::Literal {
	    value: $crate::parser::Value::Str(result), ..
	} = get!($node => $key) {
	    result
	} else {
	    panic!("expected to find value")
	}
    };
    ($node:expr => $key:ident) => {
	if let $crate::parser::AST::Literal {
	    value: $crate::parser::Value::Str(result), ..
	} = get!($node => $key) {
	    result
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
	match &*variant {
	    $(stringify!($variant) => $code,)*
		found_variant => panic!("Unexpected variant {found_variant}"),
	}
    }};
}

// Publicly export macros. This trick allows macros not to be at
// toplevel
pub(crate) use {get, match_variant, node, value};

pub trait Tree {
    fn read(ast: AST) -> Self;
}

impl<T: Tree> Tree for Option<T> {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        match_variant! {(node) {
            None => None,
            Some => Some(T::read(get!(node => value))),
        }}
    }
}

#[derive(Debug)]
struct NonemptyVec<T>(Vec<T>);

impl<T: Tree> Tree for NonemptyVec<T> {
    fn read(mut ast: AST) -> Self {
        let mut result = Vec::new();
        loop {
            let mut node = node!(ast);
            match_variant! {(node) {
            Cons => {
                result.push(T::read(get!(node => head)));
                ast = get!(node => tail);
            },
            Nil => {
                result.push(T::read(get!(node => head)));
                break Self(result);
            },
            }}
        }
    }
}

impl<T: Tree> Tree for Vec<T> {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Option::<NonemptyVec<_>>::read(get!(node => value))
            .map(|x| x.0)
            .unwrap_or_default()
    }
}

impl Tree for () {
    fn read(ast: AST) -> Self {
        let _node = node!(ast);
    }
}
