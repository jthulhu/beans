use std::{collections::HashMap, rc::Rc};

use super::AST;
use crate::typed::{get, match_variant, node, value, Tree};

#[derive(Debug, Clone)]
pub(super) struct Ast {
    pub decls: Vec<ToplevelDeclaration>,
}

impl Tree for Ast {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self {
            decls: Vec::read(get!(node => decls)),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ToplevelDeclaration {
    Decl(Box<Declaration>),
    Macro(Box<MacroDeclaration>),
}

impl Tree for ToplevelDeclaration {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        match_variant! {(node) {
            Decl => Self::decl(Declaration::read(get!(node => decl))),
            Macro => Self::r#macro(MacroDeclaration::read(get!(node => decl))),
        }}
    }
}

impl ToplevelDeclaration {
    fn decl(decl: Declaration) -> Self {
        Self::Decl(Box::new(decl))
    }

    fn r#macro(decl: MacroDeclaration) -> Self {
        Self::Macro(Box::new(decl))
    }
}

#[derive(Debug, Clone)]
pub(super) struct MacroDeclaration {
    pub name: Rc<str>,
    pub args: Vec<Rc<str>>,
    pub rules: Vec<Rule>,
}

impl Tree for MacroDeclaration {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self {
            name: value!(node => name),
            args: Vec::read(get!(node => args))
                .into_iter()
                .map(|fa: FormalArgument| fa.0)
                .collect(),
            rules: Vec::read(get!(node => rules)),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct FormalArgument(Rc<str>);

impl Tree for FormalArgument {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self(value!(node => name))
    }
}

#[derive(Debug, Clone)]
pub(super) struct Comment(Rc<str>);

impl Tree for Comment {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self(value!(node => through))
    }
}

#[derive(Debug, Clone)]
pub(super) struct Declaration {
    pub comment: Option<Rc<str>>,
    pub axiom: bool,
    pub name: Rc<str>,
    pub rules: Vec<Rule>,
}

impl Tree for Declaration {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self {
            comment: Option::read(get!(node => comment)).map(|x: Comment| x.0),
            axiom: Option::<At>::read(get!(node => axiom)).is_some(),
            rules: Vec::read(get!(node => rules)),
            name: value!(node => name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct At;

impl Tree for At {
    fn read(ast: AST) -> Self {
        Self
    }
}

#[derive(Debug, Clone)]
pub(super) struct Rule {
    pub elements: Vec<Element>,
    pub proxy: Proxy,
    pub left_associative: bool,
}

impl Tree for Rule {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self {
            elements: Vec::read(get!(node => elements)),
            proxy: Proxy::read(get!(node => proxy)),
            left_associative: !matches!(
                Option::read(get!(node => assoc)),
                Some(Associativity::Right),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

impl Tree for Associativity {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        match_variant! {(node) {
            Left => Self::Left,
            Right => Self::Right,
        }}
    }
}

#[derive(Debug, Clone)]
pub(super) struct Element {
    pub item: Item,
    pub attribute: Option<Attribute>,
    pub key: Option<Key>,
}

impl Tree for Element {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self {
            item: Item::read(get!(node => item)),
            attribute: Option::read(get!(node => attribute)),
            key: Option::read(get!(node => key)),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum Item {
    SelfNonTerminal,
    Regular { name: Rc<str> },
    MacroInvocation { name: Rc<str>, arguments: Vec<Item> },
}

impl Tree for Item {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        match_variant! {(node) {
        SelfNonTerminal => Self::SelfNonTerminal,
            Regular => Self::Regular { name: value!(node => name) },
            MacroInvocation => Self::MacroInvocation {
            name: value!(node => name),
            arguments: Vec::read(get!(node => args)),
            }
        }}
    }
}

#[derive(Debug, Clone)]
pub(super) struct Attribute {
    pub attribute: Rc<str>,
    pub named: bool,
}

impl Tree for Attribute {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        let named = match_variant! [(node) {
            Named => true,
            Indexed => false,
        }];
        Self {
            attribute: value!(node => attribute),
            named,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct Key(pub Rc<str>);

impl Tree for Key {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self(value!(node => key))
    }
}

#[derive(Debug, Clone)]
pub(super) struct Proxy {
    pub variant: Option<Rc<str>>,
    pub items: HashMap<Rc<str>, Expression>,
}

impl Tree for Proxy {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        let vec_items: Vec<ProxyItem> = Vec::read(get!(node => through));
        let mut items = HashMap::new();
        let mut variant = None;
        for item in vec_items {
            match item {
                ProxyItem::Variant(var) => variant = Some(var),
                ProxyItem::Entry { key, value } => drop(items.insert(key, value)),
            }
        }
        Proxy { variant, items }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ProxyItem {
    Variant(Rc<str>),
    Entry { key: Rc<str>, value: Expression },
}

impl Tree for ProxyItem {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        match_variant! {(node) {
            Variant => Self::Variant(value!(node => var)),
            Entry => Self::Entry {
            key: value!(node => key),
            value: Expression::read(get!(node => value)),
            }
        }}
    }
}

#[derive(Debug, Clone)]
pub(super) enum Expression {
    String(Rc<str>),
    Id(Rc<str>),
    Instanciation {
        name: Rc<str>,
        children: HashMap<Rc<str>, Expression>,
        variant: Option<Rc<str>>,
    },
}

impl Tree for Expression {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        let res = match_variant! {(node) {
            String => Self::String(value!(node => value)),
        Id => Self::Id(value!(node => name)),
            Instanciation => {
            let mut variant = None;
            let mut children = HashMap::new();
            for item in Vec::read(get!(node => children)) {
                match item {
                ProxyItem::Variant(var) => variant = Some(var),
                ProxyItem::Entry { key, value } =>
                    drop(children.insert(key, value)),
                }
            }
            Self::Instanciation {
                name: value!(node => name),
                children,
                variant,
            }
            }
        }};
        res
    }
}
