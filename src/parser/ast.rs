use std::{collections::HashMap, rc::Rc};

use super::AST;
use crate::{
    error::{ErrorKind, Result},
    span::Span,
    typed::{get, match_variant, node, span, spanned_value, value, Spanned, Tree},
};

#[derive(Debug, Clone)]
pub(super) struct Ast {
    pub decls: Vec<Spanned<ToplevelDeclaration>>,
    pub span: Span,
}

impl Tree for Ast {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            decls: get!(node => decls).to_tree::<Spanned<_>>()?.inner,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) enum ToplevelDeclaration {
    Decl(Box<Declaration>),
    Macro(Box<MacroDeclaration>),
}

impl Tree for Spanned<ToplevelDeclaration> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(match_variant! {(node) {
            Decl => ToplevelDeclaration::decl(get!(node => decl).to_tree()?),
            Macro => ToplevelDeclaration::r#macro(get!(node => decl).to_tree()?),
        }})
    }

    fn span(&self) -> &Span {
        &self.span
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
    pub name: Spanned<Rc<str>>,
    pub args: Vec<Spanned<Rc<str>>>,
    pub rules: Vec<Rule>,
    pub span: Span,
}

impl Tree for MacroDeclaration {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            name: spanned_value!(node => name),
            args: get!(node => args)
                .to_tree::<Spanned<Vec<_>>>()?
                .inner
                .into_iter()
                .map(|fa: FormalArgument| fa.0)
                .collect(),
            rules: get!(node => rules).to_tree::<Spanned<_>>()?.inner,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct FormalArgument(Spanned<Rc<str>>);

impl Tree for FormalArgument {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self(spanned_value!(node => name)))
    }

    fn span(&self) -> &Span {
        &self.0.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct Comment(Spanned<Rc<str>>);

impl Tree for Comment {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self(spanned_value!(node => through)))
    }

    fn span(&self) -> &Span {
        &self.0.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct Declaration {
    pub comment: Option<Spanned<Rc<str>>>,
    pub axiom: Spanned<bool>,
    pub name: Spanned<Rc<str>>,
    pub rules: Vec<Rule>,
    pub span: Span,
}

impl Tree for Declaration {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            comment: get!(node => comment)
                .to_tree::<Spanned<Option<Comment>>>()?
                .transpose()
                .map(|x| x.map(|y| y.0).merge()),
            axiom: get!(node => axiom).to_tree()?,
            rules: get!(node => rules).to_tree::<Spanned<_>>()?.inner,
            name: spanned_value!(node => name),
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct Rule {
    pub elements: Vec<Element>,
    pub proxy: Proxy,
    pub left_associative: Option<Spanned<Associativity>>,
    pub span: Span,
}

impl Tree for Rule {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            elements: get!(node => elements).to_tree::<Spanned<_>>()?.inner,
            proxy: get!(node => proxy).to_tree()?,
            left_associative: get!(node => assoc).to_tree::<Spanned<_>>()?.inner,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
}

impl Tree for Spanned<Associativity> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(match_variant! {(node) {
            Left => Associativity::Left,
            Right => Associativity::Right,
        }})
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl From<Associativity> for bool {
    fn from(assoc: Associativity) -> bool {
	matches!(assoc, Associativity::Left)
    }
}

#[derive(Debug, Clone)]
pub(super) struct Element {
    pub item: Spanned<Item>,
    pub attribute: Option<Attribute>,
    pub key: Option<Key>,
    pub span: Span,
}

impl Tree for Element {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            item: get!(node => item).to_tree()?,
            attribute: get!(node => attribute).to_tree::<Spanned<_>>()?.inner,
            key: get!(node => key).to_tree::<Spanned<_>>()?.inner,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) enum Item {
    SelfNonTerminal,
    Regular {
        name: Spanned<Rc<str>>,
    },
    MacroInvocation {
        name: Spanned<Rc<str>>,
        arguments: Vec<Spanned<Item>>,
    },
}

impl Tree for Spanned<Item> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(match_variant! {(node) {
            SelfNonTerminal => Item::SelfNonTerminal,
            Regular => Item::Regular { name: spanned_value!(node => name) },
            MacroInvocation => Item::MacroInvocation {
		name: spanned_value!(node => name),
		arguments: get!(node => args).to_tree::<Spanned<_>>()?.inner,
            }
        }})
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct Attribute {
    pub attribute: Spanned<Rc<str>>,
    pub named: Spanned<bool>,
    pub span: Span,
}

impl Tree for Attribute {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        let named = match_variant! [(node) {
            Named => true,
            Indexed => false,
        }];
        Ok(Self {
            attribute: spanned_value!(node => attribute),
            named,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct Key(pub Spanned<Rc<str>>);

impl Tree for Key {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self(spanned_value!(node => key)))
    }

    fn span(&self) -> &Span {
        &self.0.span
    }
}

#[derive(Debug, Clone)]
pub(super) struct Proxy {
    pub variant: Option<Spanned<Rc<str>>>,
    pub items: HashMap<Rc<str>, (Spanned<Expression>, Span)>,
    pub span: Span,
}

impl Tree for Proxy {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        let vec_items: Vec<Spanned<ProxyItem>> =
            get!(node => through).to_tree::<Spanned<_>>()?.inner;
        let mut items = HashMap::new();
        let mut variant = None;
        for item in vec_items {
            match item.inner {
                ProxyItem::Variant(var) => variant = Some(var),
                ProxyItem::Entry { key, value } => {
                    if let Some((_, old_span)) =
                        items.insert(key.inner.clone(), (value, key.span.clone()))
                    {
                        return ErrorKind::GrammarDuplicateProxyItem {
                            name: key.inner.to_string(),
                            span: key.span.into(),
                            old_span: old_span.into(),
                        }
                        .err();
                    }
                }
            }
        }
        Ok(Proxy {
            variant,
            items,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) enum ProxyItem {
    Variant(Spanned<Rc<str>>),
    Entry {
        key: Spanned<Rc<str>>,
        value: Spanned<Expression>,
    },
}

impl Tree for Spanned<ProxyItem> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(match_variant! {(node) {
            Variant => ProxyItem::Variant(spanned_value!(node => var)),
            Entry => ProxyItem::Entry {
            key: spanned_value!(node => key),
            value: get!(node => value).to_tree()?,
            }
        }})
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub(super) enum Expression {
    String(Rc<str>),
    Id(Rc<str>),
    Instanciation {
        name: Spanned<Rc<str>>,
        children: HashMap<Rc<str>, (Spanned<Expression>, Span)>,
        variant: Option<Spanned<Rc<str>>>,
    },
}

impl Tree for Spanned<Expression> {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        let res = match_variant! {(node) {
            String => Expression::String(value!(node => value)),
            Id => Expression::Id(value!(node => name)),
            Instanciation => {
		let mut variant = None;
		let mut children = HashMap::new();
		for item in get!(node => children).to_tree::<Spanned<Vec<Spanned<_>>>>()?.inner {
		    match item.inner {
			ProxyItem::Variant(var) => variant = Some(var),
			ProxyItem::Entry { key, value } =>
			    if let Some((_, old_span)) = children.insert(key.inner.clone(), (value, key.span.clone())) {
				return ErrorKind::GrammarDuplicateProxyItem {
				    name: key.inner.to_string(),
				    span: key.span.into(),
				    old_span: old_span.into(),
				}
				.err();
			    }
		    }
		}
		Expression::Instanciation {
		    name: spanned_value!(node => name),
		    children,
		    variant,
		}
            }
        }};
        Ok(res)
    }

    fn span(&self) -> &Span {
        &self.span
    }
}
