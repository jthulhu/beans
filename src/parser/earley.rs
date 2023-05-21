use super::ast::{
    Ast, Attribute as AstAttribute, Element as AstElement, Expression, Item, Proxy as AstProxy,
    Rule as AstRule, ToplevelDeclaration,
};
use super::grammar::{
    Attribute, Axioms, Element, ElementType, NonTerminalDescription, NonTerminalName,
    Nullables, Proxy, Rule, RuleId, Rules, ValueTemplate,
};
use super::parser::{NonTerminalId, ParseResult, Parser, Value, AST};
use crate::typed::Spanned;
use crate::{
    build_system,
    builder::{select_format, Buildable, FileResult, Format},
    error::{Error, ErrorKind, Result},
    lexer::{Grammar as LexerGrammar, LexedStream, Lexer, TerminalId, Token},
    list::List,
    regex::Allowed,
    span::Span,
    stream::StringStream,
    typed::Tree,
};
use bincode::deserialize;
use fragile::Fragile;
use itertools::Itertools;
use newty::{newty, nvec};
use serde::{Deserialize, Serialize};
use std::cmp::{Ordering, Reverse};
use std::collections::VecDeque;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub fn print_sets(sets: &[StateSet], parser: &EarleyParser, lexer: &Lexer) {
    for (i, set) in sets.iter().enumerate() {
        println!("=== {} ===", i);
        for item in set.slice() {
            let mut line = String::new();
            let rule = &parser.grammar().rules[item.rule];
            line.push_str(&parser.grammar().name_of[rule.id]);
            line.push_str(" -> ");
            for i in 0..item.position {
                line.push_str(&rule.elements[i].name(lexer.grammar(), parser.grammar()));
                line.push(' ');
            }
            line.push_str("• ");
            for i in item.position..rule.elements.len() {
                line.push_str(&rule.elements[i].name(lexer.grammar(), parser.grammar()));
                line.push(' ');
            }
            line.extend(format!("({})", item.origin).chars());
            println!("{}", line);
        }
        println!();
    }
}

pub fn print_final_sets(sets: &[FinalSet], parser: &EarleyParser, lexer: &Lexer) {
    for (i, set) in sets.iter().enumerate() {
        println!("=== {} ===", i);
        for item in &set.set.0 {
            let rule = &parser.grammar().rules[item.rule];
            print!("{} -> ", parser.grammar().name_of[rule.id]);
            for element in rule.elements.iter() {
                print!("{}", element.name(lexer.grammar(), parser.grammar()));
                match &element.attribute {
                    Attribute::Indexed(i) => print!(".{}", i),
                    Attribute::Named(n) => print!(".{}", n),
                    Attribute::None => {}
                }
                if let Some(key) = &element.key {
                    print!("@{}", key);
                }
                print!(" ");
            }
            println!("({})", item.end);
        }
        println!();
    }
}

type Table = Vec<StateSet>;
type Forest = Vec<FinalSet>;

newty! {
    #[derive(serde::Serialize, serde::Deserialize)]
    pub vec RulesMap(Vec<RuleId>)[NonTerminalId]
}

newty! {
    pub vec IsIn(Vec<RuleId>)[NonTerminalId]
}

/// # Summary
/// `EarleyItem` is partially recognized handle.
/// If `item.rule` refers to `β → α_1…α_n`, the item is `β → α_1…α_{i-1} · α_i…α_n (j)`
/// where `i=item.position` and `j=item.origin`.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct EarleyItem {
    /// `rule` is the identifier of the associated [`Rule`].
    rule: RuleId,
    /// `origin` is the identifier of the `EarleySet` this item was originated in.
    origin: usize,
    /// `position` is the advancement of the current item. It corresponds to the position of the fat dot.
    position: usize,
    /// `parent_has_been_shown` indicates whether a parent item should be reported in case
    /// of failure. It should *not* be showed if it has no description, if it hash
    /// matched something already, or if a parent item is already a candidate
    /// for the error message.
    parent_has_been_shown: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FinalItem {
    /// `rule` is the identifier of the associated [`Rule`]
    rule: RuleId,
    end: usize,
}

impl fmt::Display for FinalItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "#{}\t\t({})", self.rule, self.end)
    }
}

/// # Summary
/// `EarleyGrammar` is a grammar that uses the Earley algorithm.
/// The general worst-time complexity for a context-free grammar is `O(n³)`.
/// For an unambiguous grammar, the worst-time complexity is `O(n²)`.
/// For an `LR(k)` grammar, if the Johnson algorithm is applied (which is currently not), the complexity is
/// `O(n)`.
/// If it is not applied, the complexity is `O(n)` unless there is right-recursion, in which case the
/// complexity is `O(n²)`.
#[derive(Serialize, Deserialize, Debug)]
pub struct EarleyGrammar {
    /// The axioms, indexed by RuleId.
    axioms: Axioms,
    /// The rules. The rule index is its identifier.
    rules: Rules,
    /// The nullables, indexed by NonTerminalId.
    nullables: Nullables,
    /// Maps the name of a non-terminal to its identifier.
    id_of: HashMap<Rc<str>, NonTerminalId>,
    /// Maps the non-terminal to its name
    name_of: NonTerminalName,
    description_of: NonTerminalDescription,
    /// Maps the identifier of a non-terminal to the identifiers of its rules.
    /// Its rules are the rules of which it is the LHS.
    rules_of: RulesMap,
}

impl EarleyGrammar {
    fn has_rules(&self, id: NonTerminalId) -> &[RuleId] {
        &self.rules_of[id]
    }
}

impl EarleyGrammar {
    pub fn new(
        rules: Rules,
        axioms: Axioms,
        id_of: HashMap<Rc<str>, NonTerminalId>,
        name_of: NonTerminalName,
        description_of: NonTerminalDescription,
    ) -> Result<Self> {
        let nb_non_terminals = axioms.len_as(); // Number of non terminals
                                                // nullables[non_term_id]: bool is whether non terminal with
                                                // this id is nullable, meaning it can match ε (empty string).
        let mut nullables = Nullables::with_capacity(nb_non_terminals);
        // rules_of[non_term_id]: [rule_id] is a Vec containing all the rules whose LHS is the non terminal
        // this id.
        let mut rules_of = nvec![RulesMap Vec::new(); nb_non_terminals];
        // is_in[non_term_id]: [rule_id] is a Vec containing all the rules whose RHS contains the non terminal
        // with this id.
        let mut is_in = nvec![IsIn Vec::new(); nb_non_terminals];
        let mut stack = VecDeque::with_capacity(is_in.len());
        for (i, rule) in rules.iter().enumerate() {
            let rule_id = RuleId(i);
            let lhs_id = rule.id;
            rules_of[lhs_id].push(rule_id);
            if rule.elements.is_empty() {
                nullables.insert(lhs_id);
                stack.push_front(lhs_id);
            }
            for element in rule.elements.iter() {
                if let ElementType::NonTerminal(rhs_id) = element.element_type {
                    is_in[rhs_id].push(rule_id);
                }
            }
        }

        while let Some(current) = stack.pop_back() {
            for &rule_id in &is_in[current] {
                let lhs_id = rules[rule_id].id;
                if !nullables.contains(lhs_id)
                    && rules[rule_id].elements.iter().all(|element| {
                        match element.element_type {
                            ElementType::NonTerminal(id) => nullables.contains(id),
                            _ => false,
                        }
                    })
                {
                    nullables.insert(lhs_id);
                    stack.push_front(lhs_id);
                }
            }
        }

        Ok(Self {
            axioms,
            rules,
            nullables,
            id_of,
            name_of,
            description_of,
            rules_of,
        })
    }

    pub fn name_of(&self, id: NonTerminalId) -> Rc<str> {
        self.name_of[id].clone()
    }

    pub fn description_of(&self, id: NonTerminalId) -> Option<Rc<str>> {
        self.description_of[id].as_ref().cloned()
    }

    pub fn id_of(&self, name: Rc<str>) -> NonTerminalId {
        self.id_of[&name]
    }
}

impl EarleyGrammar {
    const PLAIN_EXTENSION: &str = "gr";
    const COMPILED_EXTENSION: &str = "cgr";
    const AST_EXTENSION: &str = "ast";

    pub fn build_from_compiled(
        blob: &[u8],
        path: impl ToOwned<Owned = PathBuf>,
    ) -> Result<Self> {
        deserialize(blob).map_err(|error| Error::with_file(error, path.to_owned()))
    }

    pub fn build_from_ast(ast: AST, lexer_grammar: &LexerGrammar) -> Result<Self> {
        type InvokedMacros = HashMap<(Rc<str>, Rc<[ElementType]>), NonTerminalId>;
        type MacroDeclarations = HashMap<Rc<str>, (Vec<Spanned<Rc<str>>>, Vec<AstRule>, Span)>;
        type FoundNonTerminals = HashMap<Rc<str>, (NonTerminalId, Span)>;

        let typed_ast = Ast::read(ast)?;
        // `macro_declarations` holds every macro declaration found in a grammar. This will be
        // used to invoke the macros.
        //
        // It maps a macro name to a tuple containing the name of its arguments, and the rules
        // it produces.
        let mut macro_declarations = HashMap::new();
        let mut non_terminal_declarations = Vec::new();
        // `found_nonterminals` contains the *name* of the nonterminals found.
        // It's useful to decide, as soon as we find a name later on, whether it's a terminal,
        // a non-terminal, or something undefined (and therefore to raise an error).
        let mut found_nonterminals = HashMap::new();
        let mut available_id = NonTerminalId(0);
        let mut id_of = HashMap::new();
        let mut name_of = NonTerminalName::new();
        let mut description_of = NonTerminalDescription::new();

        for decl in typed_ast.decls {
            match decl.inner {
                ToplevelDeclaration::Macro(macro_decl) => {
                    if let Some((_, _, old_span)) = macro_declarations.insert(
                        macro_decl.name.inner.clone(),
                        (
                            macro_decl.args,
                            macro_decl.rules,
                            macro_decl.name.span.clone(),
                        ),
                    ) {
                        return ErrorKind::GrammarDuplicateMacroDefinition {
                            span: macro_decl.name.span.into(),
                            old_span: old_span.into(),
                            name: macro_decl.name.inner.to_string(),
                        }
                        .err();
                    }
                }
                ToplevelDeclaration::Decl(decl) => {
                    let id = available_id.next();
                    if let Some((_, old_span)) = found_nonterminals
                        .insert(decl.name.inner.clone(), (id, decl.name.span.clone()))
                    {
                        return ErrorKind::GrammarDuplicateDefinition {
                            name: decl.name.inner.to_string(),
                            span: decl.name.span.into(),
                            old_span: old_span.into(),
                        }
                        .err();
                    }
                    id_of.insert(decl.name.inner.clone(), id);
                    name_of.push(decl.name.inner.clone());
                    description_of.push(decl.comment.as_ref().map(|o| o.inner.clone()));
                    non_terminal_declarations.push((decl, id));
                }
            }
        }

        fn eval_rule(
            rule: &AstRule,
            macro_id: NonTerminalId,
            available_id: &mut NonTerminalId,
            rules: &mut Rules,
            invoked_macros: &mut InvokedMacros,
            name_of: &mut NonTerminalName,
            description_of: &mut NonTerminalDescription,
            id_of: &mut HashMap<Rc<str>, NonTerminalId>,
            found_nonterminals: &FoundNonTerminals,
            macro_declarations: &MacroDeclarations,
            scope: &HashMap<Rc<str>, ElementType>,
            lexer_grammar: &LexerGrammar,
        ) -> Result<Rule> {
            let mut new_elements = Vec::with_capacity(rule.elements.len());
            for element in rule.elements.iter() {
                let el = eval_element(
                    element,
                    macro_id,
                    available_id,
                    rules,
                    invoked_macros,
                    name_of,
                    description_of,
                    id_of,
                    found_nonterminals,
                    macro_declarations,
                    scope,
                    lexer_grammar,
                )?;
                new_elements.push(el);
            }
            let proxy = eval_proxy(
                &rule.proxy,
                found_nonterminals,
            )?;
            Ok(Rule::new(
                macro_id,
                new_elements,
                proxy,
                rule.left_associative
                    .as_ref()
                    .map(|Spanned { inner, .. }| (*inner).into())
                    .unwrap_or(true),
            ))
        }

        fn invoke_macro(
            name: Spanned<Rc<str>>,
            args: Rc<[ElementType]>,
            macro_id: NonTerminalId,
            available_id: &mut NonTerminalId,
            rules: &mut Rules,
            invoked_macros: &mut InvokedMacros,
            name_of: &mut NonTerminalName,
            description_of: &mut NonTerminalDescription,
            id_of: &mut HashMap<Rc<str>, NonTerminalId>,
            found_nonterminals: &FoundNonTerminals,
            macro_declarations: &MacroDeclarations,
            lexer_grammar: &LexerGrammar,
        ) -> Result<()> {
            let Some((arg_names, macro_rules, definition_span)) = macro_declarations.get(&name.inner) else {
		return ErrorKind::GrammarUndefinedMacro {
		    name: name.inner.to_string(),
		    span: name.span.into(),
		}
		.err();
	    };

            if args.len() != arg_names.len() {
                return ErrorKind::GrammarArityMismatch {
                    macro_name: name.inner.to_string(),
                    definition_arity: arg_names.len(),
                    call_arity: args.len(),
                    definition_span: definition_span.into(),
                    call_span: name.span.into(),
                }
                .err();
            }

            let scope = arg_names
                .iter()
                .map(|x| x.inner.clone())
                .zip(args.iter().copied())
                .collect();

            for rule in macro_rules {
                let actual_rule = eval_rule(
                    rule,
                    macro_id,
                    available_id,
                    rules,
                    invoked_macros,
                    name_of,
                    description_of,
                    id_of,
                    found_nonterminals,
                    macro_declarations,
                    &scope,
                    lexer_grammar,
                )?;
                rules.push(actual_rule);
            }
            Ok(())
        }

        fn eval_expression(
            expr: &Spanned<Item>,
            self_id: NonTerminalId,
            available_id: &mut NonTerminalId,
            rules: &mut Rules,
            invoked_macros: &mut InvokedMacros,
            name_of: &mut NonTerminalName,
            description_of: &mut NonTerminalDescription,
            id_of: &mut HashMap<Rc<str>, NonTerminalId>,
            found_nonterminals: &FoundNonTerminals,
            macro_declarations: &MacroDeclarations,
            scope: &HashMap<Rc<str>, ElementType>,
            lexer_grammar: &LexerGrammar,
        ) -> Result<ElementType> {
            let res = match &expr.inner {
                Item::SelfNonTerminal => ElementType::NonTerminal(self_id),
                Item::Regular { name } => {
                    if let Some(element) = scope.get(&name.inner) {
                        *element
                    } else if let Some((id, _)) = found_nonterminals.get(&name.inner) {
                        // The item is a non terminal
                        ElementType::NonTerminal(*id)
                    } else if let Some(id) = lexer_grammar.id(&name.inner) {
                        ElementType::Terminal(id)
                    } else {
                        return ErrorKind::GrammarUndefinedNonTerminal {
                            name: name.inner.to_string(),
                            span: name.span.clone().into(),
                        }
                        .err();
                    }
                }
                Item::MacroInvocation { name, arguments } => {
                    let mut args = Vec::new();
                    for arg in arguments {
                        let evaled = eval_expression(
                            arg,
                            self_id,
                            available_id,
                            rules,
                            invoked_macros,
                            name_of,
                            description_of,
                            id_of,
                            found_nonterminals,
                            macro_declarations,
                            scope,
                            lexer_grammar,
                        )?;
                        args.push(evaled);
                    }
                    let args: Rc<[_]> = Rc::from(args);
                    if let Entry::Vacant(e) = invoked_macros.entry((name.inner.clone(), args.clone())) {
                        let id = available_id.next();
                        let mut complete_name = name.inner.to_string();
                        complete_name.push('[');
                        complete_name.extend(
                            args.iter()
                                .map(|element| match element {
                                    ElementType::NonTerminal(id) => &*name_of[*id],
                                    ElementType::Terminal(id) => lexer_grammar.name(*id),
                                })
                                .intersperse(", "),
                        );
                        complete_name.push(']');
                        let complete_name: Rc<str> = Rc::from(complete_name);
                        id_of.insert(complete_name.clone(), id);
                        name_of.push(complete_name);
                        description_of.push(None);
                        e.insert(id);
                        invoke_macro(
                            name.clone(),
                            args.clone(),
                            id,
                            available_id,
                            rules,
                            invoked_macros,
                            name_of,
                            description_of,
                            id_of,
                            found_nonterminals,
                            macro_declarations,
                            lexer_grammar,
                        )?;
                    }
                    ElementType::NonTerminal(invoked_macros[&(name.inner.clone(), args)])
                }
            };
            Ok(res)
        }

        fn eval_element(
            element: &AstElement,
            id: NonTerminalId,
            available_id: &mut NonTerminalId,
            rules: &mut Rules,
            invoked_macros: &mut InvokedMacros,
            name_of: &mut NonTerminalName,
            description_of: &mut NonTerminalDescription,
            id_of: &mut HashMap<Rc<str>, NonTerminalId>,
            found_nonterminals: &HashMap<Rc<str>, (NonTerminalId, Span)>,
            macro_declarations: &MacroDeclarations,
            scope: &HashMap<Rc<str>, ElementType>,
            lexer_grammar: &LexerGrammar,
        ) -> Result<Element> {
            let attribute = match &element.attribute {
                Some(AstAttribute {
                    attribute,
                    named: Spanned { inner: true, .. },
                    span: _span,
                }) => Attribute::Named(attribute.inner.clone()),
                Some(AstAttribute {
                    attribute,
                    named: Spanned { inner: false, .. },
                    span,
                }) => {
                    let index =
                        attribute
                            .inner
                            .parse()
                            .map_err(|_| ErrorKind::IntegerTooBig {
                                string: attribute.inner.to_string(),
                                span: span.into(),
                            })?;
                    Attribute::Indexed(index)
                }
                None => Attribute::None,
            };
            let key = element.key.as_ref().map(|k| k.0.clone());
            let element_type = eval_expression(
                &element.item,
                id,
                available_id,
                rules,
                invoked_macros,
                name_of,
                description_of,
                id_of,
                found_nonterminals,
                macro_declarations,
                scope,
                lexer_grammar,
            )?;
            Ok(Element::new(attribute, key.map(|o| o.inner), element_type))
        }

        fn eval_proxy(
            proxy: &AstProxy,
            found_nonterminals: &FoundNonTerminals,
        ) -> Result<Proxy> {
            let mut actual_proxy = HashMap::new();
            if let Some(ref variant) = proxy.variant {
                actual_proxy.insert(
                    "variant".into(),
                    ValueTemplate::String(variant.inner.clone()),
                );
            }
            for (key, (expression, _)) in proxy.items.iter() {
                let value = match &expression.inner {
                    Expression::String(string) => ValueTemplate::String(string.clone()),
                    Expression::Id(id) => ValueTemplate::Variable(id.clone()),
                    Expression::Instanciation {
                        name,
                        children,
                        variant,
                    } => {
                        let Some((nonterminal, _)) = found_nonterminals.get(&name.inner) else {
			    return ErrorKind::GrammarUndefinedNonTerminal {
				name: name.inner.to_string(),
				span: name.span.clone().into()
			    }
			    .err(); 
			};

                        let fake_proxy = AstProxy {
                            variant: variant.as_ref().cloned(),
                            items: children.clone(),
                            span: expression.span.clone(),
                        };
                        let attributes = eval_proxy(
                            &fake_proxy,
                            found_nonterminals,
                        )?;
                        ValueTemplate::InlineRule {
                            non_terminal: *nonterminal,
                            attributes,
                        }
                    }
                };
                actual_proxy.insert(key.clone(), value);
            }
            Ok(actual_proxy)
        }

        let mut invoked_macros: InvokedMacros = HashMap::new();
        let mut found_axioms = Vec::new();
        let mut rules = Rules::new();
        let empty_scope = HashMap::new();
        for (declaration, id) in non_terminal_declarations {
            if declaration.axiom.inner {
                found_axioms.push(id);
            }
            for rule in declaration.rules {
                let parsed_rule = eval_rule(
                    &rule,
                    id,
                    &mut available_id,
                    &mut rules,
                    &mut invoked_macros,
                    &mut name_of,
                    &mut description_of,
                    &mut id_of,
                    &found_nonterminals,
                    &macro_declarations,
                    &empty_scope,
                    lexer_grammar,
                )?;
                rules.push(parsed_rule);
            }
        }
        let mut axioms = Axioms::with_capacity(available_id.next());
        for axiom in found_axioms {
            axioms.put(axiom);
        }
        let res = Self::new(rules, axioms, id_of, name_of, description_of)?;
        Ok(res)
    }

    pub fn build_from_plain(
        mut source: StringStream,
        lexer_grammar: &LexerGrammar,
    ) -> Result<Self> {
        let (lexer, parser) = build_system!(
            lexer => "parser.clx",
            parser => "parser.cgr",
        )?;
        let mut input = lexer.lex(&mut source);
        let result = parser.parse(&mut input)?;
        let grammar = Self::build_from_ast(result.tree, lexer_grammar)?;
        Ok(grammar)
    }

    pub fn build_from_path(path: &Path, lexer_grammar: &LexerGrammar) -> Result<Self> {
        let ast: AST = match select_format(
            path,
            &[
                (Self::COMPILED_EXTENSION, Format::Compiled),
                (Self::AST_EXTENSION, Format::Ast),
                (Self::PLAIN_EXTENSION, Format::Plain),
            ],
        ) {
            FileResult::Valid((actual_path, Format::Ast)) => {
                let file = File::open(&actual_path)
                    .map_err(|error| Error::with_file(error, &actual_path))?;
		bincode::deserialize_from(file)
                    .map_err(|error| Error::with_file(error, actual_path))?
            }
            FileResult::Valid((actual_path, Format::Plain)) => {
                let stream = StringStream::from_file(actual_path)?;
                let result = Self::build_from_plain(stream, lexer_grammar)?;
                return Ok(result);
            }
            FileResult::Valid((actual_path, Format::Compiled)) => {
                let mut file = File::open(&actual_path)
                    .map_err(|err| Error::with_file(err, &actual_path))?;
                let mut buffer = Vec::new();
                file.read_to_end(&mut buffer)
                    .map_err(|err| Error::with_file(err, &actual_path))?;
                let result = Self::build_from_compiled(&buffer, actual_path)?;
                return Ok(result);
            }
            FileResult::WrongExtension(extension) => {
                return ErrorKind::UnrecognisedExtension {
                    extension,
                    path: path.to_owned(),
                }
                .err();
            }
            FileResult::NonExisting => {
                return ErrorKind::GrammarNotFound {
                    path: path.to_owned(),
                }
                .err();
            }
        };
        let parser = Self::build_from_ast(ast, lexer_grammar)?;
        Ok(parser)
    }

    pub fn build_from_blob(
        blob: &[u8],
        path: &Path,
        lexer_grammar: &LexerGrammar,
    ) -> Result<Self> {
        let ast: AST = match select_format(
            path,
            &[
                (Self::COMPILED_EXTENSION, Format::Compiled),
                (Self::AST_EXTENSION, Format::Ast),
                (Self::PLAIN_EXTENSION, Format::Plain),
            ],
        ) {
            FileResult::Valid((actual_path, Format::Compiled)) => {
                let result = Self::build_from_compiled(blob, actual_path)?;
                return Ok(result);
            }
            FileResult::Valid((actual_path, Format::Ast)) => {
		bincode::deserialize(blob)
		    .map_err(|error| Error::with_file(error, actual_path))?
            }
            FileResult::Valid((actual_path, Format::Plain)) => {
                let string = String::from_utf8(blob.to_vec())
		    .map_err(|error| Error::with_file(error, &actual_path))?;
                let stream = StringStream::new(actual_path, string);
                let result = Self::build_from_plain(stream, lexer_grammar)?;
                return Ok(result);
            }
            FileResult::NonExisting => {
                return ErrorKind::GrammarNotFound {
                    path: path.to_owned(),
                }
                .err();
            }
            FileResult::WrongExtension(extension) => {
                return ErrorKind::UnrecognisedExtension {
                    extension,
                    path: path.to_owned(),
                }
                .err();
            }
        };
        let grammar = Self::build_from_ast(ast, lexer_grammar)?;
        Ok(grammar)
    }
}

newty! {
    id FinalItemId
}
newty! {
    #[derive(PartialEq, Eq, Clone)]
    vec FinalSetVec(FinalItem)[FinalItemId]
}
newty! {
    #[derive(Clone)]
    map FinalSetIndex(Vec<FinalItemId>)[NonTerminalId]
}

#[derive(Default, Debug, Clone, Eq)]
pub struct FinalSet {
    /// An index mapping a nonterminal to every item in the set derived from that nonterminal.
    index: FinalSetIndex,
    /// The set of items.
    set: FinalSetVec,
    /// The starting position of every item in this set, in the raw input.
    position: usize,
}

impl PartialEq for FinalSet {
    fn eq(&self, rhs: &FinalSet) -> bool {
        self.set == rhs.set && self.position == rhs.position
    }
}

impl FinalSet {
    fn add(&mut self, item: FinalItem, grammar: &EarleyGrammar) {
        self.index
            .0
            .entry(grammar.rules[item.rule].id)
            .or_default()
            .push(self.set.len_as());
        self.set.push(item);
    }

    fn iter(&self) -> impl Iterator<Item = &FinalItem> + '_ {
        self.set.iter()
    }
}

impl std::fmt::Display for FinalSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(
            f,
            r"== ({}) ==
{}",
            self.position,
            self.set
                .iter()
                .map(|item| format!("{}", item))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Default, Debug)]
pub struct StateSet {
    cache: HashSet<EarleyItem>,
    set: Vec<EarleyItem>,
    position: usize,
}

impl StateSet {
    fn add(&mut self, item: EarleyItem) {
        if !self.cache.contains(&item) {
            self.cache.insert(item);
            self.set.push(item);
        }
    }

    fn next(&mut self) -> Option<&EarleyItem> {
        if let Some(e) = self.set.get(self.position) {
            self.position += 1;
            Some(e)
        } else {
            None
        }
    }

    fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    fn slice(&self) -> &[EarleyItem] {
        &self.set
    }

    fn iter(&self) -> impl Iterator<Item = &EarleyItem> + '_ {
        self.set.iter()
    }
}

#[derive(Clone, Debug)]
struct SyntaxicItem {
    kind: SyntaxicItemKind,
    start: usize,
    end: usize,
}

#[derive(Clone, Debug)]
enum SyntaxicItemKind {
    Rule(RuleId),
    Token(Token),
}

// #[derive(Debug, Clone)]
// struct ScanItem<'a> {
//     item: &'a FinalItem,
//     depth: usize,
//     nodes_so_far: Vec<AST>,
// }

// #[derive(Debug)]
// struct SearchItem<'a> {
//     /// Index of the next scan in the raw input.
//     position: usize,
//     /// Current item to be searched for.
//     current: ScanItem<'a>,
//     /// Items scanned so far.
//     stack: Vec<ScanItem<'a>>,
// }

// #[derive(Debug)]
// struct ScItem {
//     rule: usize,
//     start: usize,
//     end: usize,
//     depth: usize,
// }

// #[derive(Debug)]
// struct SItem {
//     current: ScItem,
//     items: Vec<ScItem>,
// }

/// # Summary
/// [`EarleyParser`] is the parser related to the [`EarleyGrammar`](EarleyGrammar).
#[derive(Debug)]
pub struct EarleyParser {
    grammar: EarleyGrammar,
}

impl EarleyParser {
    pub fn build_from_blob(
        blob: &[u8],
        path: &Path,
        lexer_grammar: &LexerGrammar,
    ) -> Result<Self> {
        let grammar = EarleyGrammar::build_from_blob(blob, path, lexer_grammar)?;
        Ok(Self { grammar })
    }

    fn find_children(
        &self,
        element: SyntaxicItem,
        forest: &[FinalSet],
        raw_input: &[Token],
    ) -> Vec<SyntaxicItem> {
        match element.kind {
            SyntaxicItemKind::Rule(rule) => {
                let mut boundary = vec![(List::default(), element.start)];
                for elem in self.grammar.rules[rule].elements.iter() {
                    let mut next_boundary = Vec::new();
                    for (children, curpos) in boundary.drain(..) {
                        match elem.element_type {
                            ElementType::NonTerminal(id) => {
                                if let Some(rules) = forest[curpos].index.get(&id) {
                                    for final_item in rules
                                        .iter()
                                        .map(|&rule| &forest[curpos].set[rule])
                                        .filter(|final_item| final_item.end <= element.end)
                                    {
                                        next_boundary.push((
                                            children.cons(SyntaxicItem {
                                                kind: SyntaxicItemKind::Rule(final_item.rule),
                                                start: curpos,
                                                end: final_item.end,
                                            }),
                                            final_item.end,
                                        ))
                                    }
                                }
                            }
                            ElementType::Terminal(id)
                                if curpos < element.end && raw_input[curpos].id() == id =>
                            {
                                next_boundary.push((
                                    children.cons(SyntaxicItem {
                                        kind: SyntaxicItemKind::Token(
                                            raw_input[curpos].clone(),
                                        ),
                                        start: curpos,
                                        end: curpos + 1,
                                    }),
                                    curpos + 1,
                                ))
                            }
                            _ => {}
                        }
                    }
                    boundary.extend(next_boundary.into_iter().rev());
                }
                let children = boundary
                    .into_iter()
                    .filter_map(|(children, pos)| {
                        if pos == element.end {
                            Some(children)
                        } else {
                            None
                        }
                    })
                    .max_by(|left_children, right_children| {
                        for (left, right) in left_children.iter().zip(right_children.iter()) {
                            let SyntaxicItemKind::Rule(left_rule) = left.kind else {
				continue;
			    };
                            let SyntaxicItemKind::Rule(right_rule) = right.kind else {
				continue;
			    };
                            let assoc_ord = if self.grammar.rules[rule].left_associative {
                                left.start.cmp(&right.start)
                            } else {
                                right.start.cmp(&left.start)
                            };
                            let ord = match assoc_ord {
                                Ordering::Equal => left_rule.cmp(&right_rule),
                                other => other,
                            };
                            match ord {
                                Ordering::Equal => continue,
                                other => return other,
                            }
                        }
                        Ordering::Equal
                    })
                    .unwrap();
                children
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                    .collect()
            }
            SyntaxicItemKind::Token(_) => Vec::new(),
        }
    }

    fn build_ast(
        &self,
        item: SyntaxicItem,
        forest: &[FinalSet],
        raw_input: &[Token],
        last_span: &Span,
    ) -> AST {
        match item.kind {
            SyntaxicItemKind::Rule(rule) => {
                let span = if raw_input.is_empty() {
                    last_span.clone()
                } else if item.end == item.start {
                    raw_input[item.start].span().clone()
                } else {
                    raw_input[item.start]
                        .span()
                        .sup(raw_input[item.end - 1].span())
                };
                let all_attributes = self
                    .find_children(item, forest, raw_input)
                    .into_iter()
                    .map(|item| self.build_ast(item, forest, raw_input, last_span))
                    .zip(self.grammar.rules[rule].elements.iter())
                    .filter_map(|(item, element)| {
                        element.key.as_ref().map(|key| match &element.attribute {
                            Attribute::Named(attr) => {
                                let AST::Node { attributes, .. } = item else {
				    unreachable!("{item:?}.{attr}")
				};
                                (key.clone(), attributes[attr].clone())
                            }
                            Attribute::Indexed(idx) => {
                                let AST::Terminal(token) = item else {
				    unreachable!("{item:?}.{idx}")
				};
                                (
                                    key.clone(),
                                    AST::Literal {
                                        value: Value::Str(Rc::from(
                                            token.attributes()[idx].as_str(),
                                        )),
                                        span: Some(token.span().clone()),
                                    },
                                )
                            }
                            Attribute::None => (key.clone(), item),
                        })
                    })
                    .collect::<HashMap<Rc<str>, _>>();
                let mut removed: HashSet<Rc<str>> = HashSet::new();
                let nonterminal = self.grammar.rules[rule].id;
                let mut attributes: HashMap<_, _> = self.grammar.rules[rule]
                    .proxy
                    .iter()
                    .map(|(key, wanted)| {
                        (
                            key.clone(),
                            wanted.evaluate(&all_attributes, &mut removed, &span),
                        )
                    })
                    .collect();
                attributes.extend(
                    all_attributes
                        .into_iter()
                        .filter(|(key, _)| !removed.contains(key)),
                );
                AST::Node {
                    nonterminal,
                    attributes,
                    span,
                }
            }
            SyntaxicItemKind::Token(token) => AST::Terminal(token),
        }
    }

    /// Select one AST, assuming there is one.
    pub fn select_ast(
        &self,
        forest: &[FinalSet],
        raw_input: &[Token],
        last_span: &Span,
    ) -> AST {
        forest[0]
            .iter()
            .filter(|item| {
                item.end == raw_input.len()
                    && self
                        .grammar
                        .axioms
                        .contains(self.grammar.rules[item.rule].id)
            })
            .sorted_unstable_by_key(|item| Reverse(item.rule))
            .map(|item| SyntaxicItem {
                start: 0,
                end: raw_input.len(),
                kind: SyntaxicItemKind::Rule(item.rule),
            })
            .map(|item| self.build_ast(item, forest, raw_input, last_span))
            .next()
            .unwrap()
    }

    pub fn to_forest(&self, table: &[StateSet], raw_input: &[Token]) -> Result<Forest> {
        let mut forest = vec![FinalSet::default(); table.len()];
        for (i, set) in table.iter().enumerate() {
            forest[i].position = i;
            if set.is_empty() {
                return ErrorKind::InternalError {
                    message: format!(
                        "While creating the forest, could not find any item in set {}, at {}",
                        i,
                        raw_input[i].span(),
                    ),
                }
                .err();
            }
            set.iter()
                .filter(|item| item.position == self.grammar.rules[item.rule].elements.len())
                .for_each(|item| {
                    forest[item.origin].add(
                        FinalItem {
                            end: i,
                            rule: item.rule,
                        },
                        &self.grammar,
                    )
                });
        }
        Ok(forest)
    }

    pub fn recognise<'input, 'linput: 'input>(
        &self,
        input: &'input mut LexedStream<'linput, 'linput>,
    ) -> Result<(Table, Vec<Token>)> {
        let mut sets = Vec::new();
        let mut first_state = StateSet::default();
        let mut possible_first_nonterminals = HashSet::new();
        let mut possible_first_terminals = HashSet::new();
        (0..self.grammar().rules.len())
            .map(RuleId)
            .filter(|id| self.grammar.axioms.contains(self.grammar.rules[*id].id))
            .for_each(|id| {
                let parent_has_been_shown = if let Some(description) =
                    self.grammar().description_of(self.grammar().rules[id].id)
                {
                    possible_first_nonterminals.insert(description);
                    true
                } else {
                    false
                };

                first_state.add(EarleyItem {
                    rule: id,
                    origin: 0,
                    position: 0,
                    parent_has_been_shown,
                })
            });
	
        let mut raw_input = Vec::new();
        sets.push(first_state);
        let mut pos = 0;
        'outer: loop {
            let mut next_state = StateSet::default();
            let mut scans: HashMap<TerminalId, Vec<_>> = HashMap::new();
            '_inner: while let Some(&item) = sets.last_mut().unwrap().next() {
                let mut to_be_added = Vec::new();
                match self.grammar().rules[item.rule].elements.get(item.position) {
                    Some(element) => match element.element_type {
                        // Prediction
                        ElementType::NonTerminal(id) => {
                            for &rule in self.grammar().has_rules(id) {
                                let parent_has_been_shown = item.parent_has_been_shown
                                    || if let Some(description) = self
                                        .grammar
                                        .description_of(self.grammar().rules[rule].id)
                                    {
                                        possible_first_nonterminals.insert(description);
                                        true
                                    } else {
                                        false
                                    };
                                to_be_added.push(EarleyItem {
                                    rule,
                                    origin: pos,
                                    position: 0,
                                    parent_has_been_shown,
                                });
                            }
                            if self.grammar.nullables.contains(id) {
                                to_be_added.push(EarleyItem {
                                    rule: item.rule,
                                    origin: item.origin,
                                    position: item.position + 1,
                                    parent_has_been_shown: item.parent_has_been_shown,
                                });
                            }
                        }
                        // Scan
                        ElementType::Terminal(id) => {
                            if !item.parent_has_been_shown {
                                if let Some(message) =
                                    input.lexer().grammar().description_of(id)
                                {
                                    possible_first_terminals.insert(message.to_string());
                                } else {
                                    possible_first_terminals
                                        .insert(input.lexer().grammar().name(id).to_string());
                                };
                            }
                            scans.entry(id).or_default().push(EarleyItem {
                                rule: item.rule,
                                origin: item.origin,
                                position: item.position + 1,
                                parent_has_been_shown: false,
                            })
                        }
                    },
                    // Completion
                    None => {
                        for &parent in sets[item.origin].slice() {
                            if let Some(Element {
                                element_type: ElementType::NonTerminal(nonterminal),
                                ..
                            }) = self.grammar().rules[parent.rule]
                                .elements
                                .get(parent.position)
                            {
                                if *nonterminal == self.grammar().rules[item.rule].id {
                                    to_be_added.push(EarleyItem {
                                        rule: parent.rule,
                                        origin: parent.origin,
                                        position: parent.position + 1,
                                        parent_has_been_shown: item.parent_has_been_shown,
                                    })
                                }
                            }
                        }
                    }
                }
                for item in to_be_added {
                    sets.last_mut().unwrap().add(item);
                }
            }

            let possible_scans = input
                .lexer()
                .grammar()
                .default_allowed()
                .chain(scans.keys().cloned())
                .collect::<Vec<_>>();
            let allowed = Allowed::Some(possible_scans.clone());
            let next_token = match input.next(allowed) {
                Ok(r) => r,
                Err(error) => {
                    if let ErrorKind::LexingError { .. } = *error.kind {
                        let error = if let Some(token) = input.next(Allowed::All)? {
                            let span = token.span().clone();
                            let name = {
                                let id = token.id();
                                let name = token.name().to_string();
                                if let Some(description) =
                                    input.lexer().grammar().description_of(id)
                                {
                                    description.to_string()
                                } else {
                                    name
                                }
                            };
                            ErrorKind::SyntaxError {
                                name,
                                alternatives: possible_first_nonterminals
                                    .drain()
                                    .map(|x| x.to_string())
                                    .chain(possible_first_terminals.drain())
                                    .collect(),
                                span: Fragile::new(span),
                            }
                        } else {
                            ErrorKind::SyntaxErrorValidPrefix {
                                span: input.last_span().into(),
                            }
                        };
                        return error.err();
                    } else {
                        return Err(error);
                    }
                }
            };
            possible_first_nonterminals.clear();
            possible_first_terminals.clear();
            if let Some(token) = next_token {
                for item in scans.entry(token.id()).or_default() {
                    next_state.add(*item);
                }
                raw_input.push(token.clone());
            } else if sets.last().unwrap().set.iter().any(|item| {
                let rule = &self.grammar.rules[item.rule];
                item.origin == 0
                    && self.grammar.axioms.contains(rule.id)
                    && rule.elements.len() == item.position
            }) {
                break 'outer Ok((sets, raw_input));
            } else {
                return ErrorKind::SyntaxErrorValidPrefix {
                    span: input.last_span().into(),
                }
                .err();
            };

            sets.push(next_state);
            pos += 1;
        }
    }
}

impl Parser<'_> for EarleyParser {
    type Grammar = EarleyGrammar;

    fn new(grammar: Self::Grammar) -> Self {
        Self { grammar }
    }

    fn grammar(&self) -> &Self::Grammar {
        &self.grammar
    }

    fn is_valid<'input>(&self, input: &'input mut LexedStream<'input, 'input>) -> bool {
        self.recognise(input).is_ok()
    }

    fn parse<'input>(
        &self,
        input: &'input mut LexedStream<'input, 'input>,
    ) -> Result<ParseResult> {
        let (table, raw_input) = self.recognise(input)?;
        let forest = self.to_forest(&table, &raw_input)?;
        // print_final_sets(&forest, self);
        let tree = self.select_ast(&forest, &raw_input, input.last_span());
        Ok(ParseResult { tree })
    }
}

// impl Buildable for EarleyParser {
//     const RAW_EXTENSION: &'static str = "gr";
//     const COMPILED_EXTENSION: &'static str = "cgr";

//     fn build_from_ast(ast: AST) -> Result<Self> {
//         EarleyGrammar::build_from_ast(ast).map(|ws| ws.map(Self::new))
//     }

//     fn build_from_compiled(blob: &[u8]) -> Result<Self> {
//         EarleyGrammar::build_from_compiled(blob).map(|ws| ws.map(Self::new))
//     }

//     fn build_from_plain(raw: StringStream) -> Result<Self> {
//         EarleyGrammar::build_from_plain(raw).map(|ws| ws.map(Self::new))
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    const GRAMMAR_NUMBERS_LEXER: &str = r#"
NUMBER ::= ([0-9])
PM ::= [-+]
TD ::= [*/]
LPAR ::= \(
RPAR ::= \)
"#;

    const GRAMMAR_NUMBERS: &str = r#"
@Sum ::= Sum@left PM Product@right <>
 Product@self <>;

Product ::= Product@left TD Factor@right <>
 Factor.self@self <>;

Factor ::= LPAR Sum@self RPAR <>
 NUMBER.0@self <>;"#;

    const GRAMMAR_PROXY_LEXER: &str = r#"
NUMBER ::= ([0-9]+)
OP ::= \+
LPAR ::= \(
RPAR ::= \)
"#;
    const GRAMMAR_NUMBERS_IMPROVED: &str = r#"
@Expr ::=
  NUMBER.0@value <Literal>
  (left-assoc) Expr@left TD Expr@right <MulDiv>
  (right-assoc) Expr@left PM Expr@right <AddSub>
  LPAR Expr@value RPAR <Through>;
"#;

    const GRAMMAR_PROXY: &str = r#"
@Expression ::=
  NUMBER.0@value <Literal>
  Expression@left OP Expression@right <Operation>
  OP Expression@right <Operation, left: Expression {Literal, value: "0"}>
  LPAR Expression@value RPAR <Parenthesized>;
"#;
    const GRAMMAR_PROXY_WRONG_1: &str = r#"
@Expression ::=
  NUMBER.0@value <Literal>
  Expression@left OP Expression@right <Operation>
  OP Expression@right <
    Operation,
    left: Expression {variant: Literal value: "0"}
  >
  LPAR Expression@value RPAR <Parenthesized>;
"#;
    const GRAMMAR_PROXY_WRONG_2: &str = r#"
@Expression ::=
  NUMBER.0@value <Literal>
  Expression@left OP Expression@right <Operation>
  OP Expression@right <
    Operation
    left: Expression {Literal value: "0"}
  >
  LPAR Expression@value RPAR <Parenthesized>;
"#;

    const GRAMMAR_C_LEXER: &str = include_str!("gmrs/petitc.lx");
    const GRAMMAR_C: &str = include_str!("gmrs/petitc.gr");

    struct TestEarleyItem {
        name: &'static str,
        left_elements: Vec<&'static str>,
        right_elements: Vec<&'static str>,
        origin: usize,
    }

    impl TestEarleyItem {
        fn matches(
            &self,
            other: &EarleyItem,
            parser: &EarleyParser,
            lexer: &Lexer,
            set_id: usize,
            item_id: usize,
        ) {
            let error_message = format!("Set #{}, item #{}: no match:", set_id, item_id);
            let item = &parser.grammar().rules[other.rule];
            assert_eq!(
                self.name,
                &*parser.grammar().name_of[item.id],
                "{} name.",
                error_message
            );
            assert_eq!(
                self.left_elements.len() + self.right_elements.len(),
                item.elements.len(),
                "{} origin set.\nExpected: [{:?}, {:?}]\nGot: {:?}",
                error_message,
                self.left_elements,
                self.right_elements,
                item.elements,
            );
            assert_eq!(
                self.left_elements.len(),
                other.position,
                "{} fat dot position.",
                error_message,
            );
            assert_eq!(self.origin, other.origin, "{} origin set.", error_message);
            for i in 0..self.left_elements.len() {
                assert_eq!(
                    self.left_elements[i],
                    &*item.elements[i].name(lexer.grammar(), parser.grammar()),
                    "{} element #{}.",
                    error_message,
                    i
                );
            }
            for i in 0..self.right_elements.len() {
                assert_eq!(
                    self.right_elements[i],
                    &*item.elements[i + other.position].name(lexer.grammar(), parser.grammar()),
                    "{} elements #{}.",
                    error_message,
                    i + other.position
                );
            }
        }
    }

    /// `sets!` eases the creation of mock Earley Sets.
    /// Useful for tests.
    ///
    /// The syntax is aimed to be simple and intuitive, matching the one
    /// usually used in the literature.
    macro_rules! sets {
	(
	    $(
		== $(
		    $name: ident -> $($left_element: ident)* . $($right_element: ident)* ($origin: literal)
		)*
	    )*
	) => {
	    {
		#[allow(unused_mut)]
		let mut sets = Vec::new();
		$(
		    #[allow(unused_mut)]
		    let mut set = Vec::new();
		    $(
			set.push(earley_item!($name -> $($left_element)* . $($right_element)* ($origin)));
		)*
			sets.push(set);
		)*
		    sets
	    }
	};
    }

    /// `earley_item` creates mock Earley Items.
    /// Useful for tests.
    macro_rules! earley_item {
	($name: ident -> $($left_element: ident)* . $($right_element: ident)* ($origin: literal)) => {
	    {
		let left_elements = vec![$(
		    stringify!($left_element)
		),*];
		let right_elements = vec![$(
		    stringify!($right_element)
		),*];
		TestEarleyItem {
		    name: stringify!($name),
		    left_elements,
		    right_elements,
		    origin: $origin
		}
	    }
	};
    }

    macro_rules! final_sets {
	(
	    ($grammar:expr)
            ($lexer:expr)
	    $(
		== $(
		    $name: ident -> $($element: ident)* ($end: literal)
		)*
	    )*
	) => {{
	    #[allow(unused_mut)]
	    let mut sets = Vec::new();
	    fn find_item(grammar: &EarleyGrammar, lexer_grammar: &$crate::lexer::Grammar, name: &str, elements: &[&str], end: usize) -> FinalItem {
		for &rule_identifier in grammar
		    .id_of
		    .get(&Rc::from(name))
		    .map(|&identifier| &grammar.rules_of[identifier])
		    .expect(format!("The non-terminal {} does not exist.", name).as_str())
		    .iter()
		{
		    if elements.len() == grammar
			.rules[rule_identifier]
			.elements.len()
			&& elements
			.iter()
			.zip(grammar.rules[rule_identifier].elements.iter())
			.all(|(&left, right)| left == &*right.name(lexer_grammar, grammar))
		    {
			return FinalItem {
			    rule: rule_identifier,
			    end
			};
		    }
		}
		panic!("The rule {} -> {} is not in the grammar.", name, elements.join(" "));
	    }
	    $(
		#[allow(unused_mut)]
		let mut set = FinalSet::default();
		set.position = sets.len();
		$(
		    set.add(find_item($grammar, $lexer, stringify!($name), &[$(stringify!($element)),*], $end), $grammar);
		)*
		sets.push(set);
	    )*
	    sets
	}};
    }

    #[derive(Debug, Clone)]
    struct TestToken {
        name: &'static str,
        attributes: Vec<(usize, &'static str)>,
    }

    impl PartialEq<Token> for TestToken {
        fn eq(&self, other: &Token) -> bool {
            other.name() == self.name
                && other
                    .attributes()
                    .iter()
                    .map(|(&key, value)| (key, value.as_str()))
                    .collect::<Vec<_>>()
                    == self.attributes
        }
    }

    #[derive(Clone)]
    struct MapVec(Vec<(&'static str, TestAST)>);

    impl std::fmt::Debug for MapVec {
        fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            formatter
                .debug_map()
                .entries(self.0.iter().map(|(k, v)| (k, v)))
                .finish()
        }
    }

    impl From<Vec<(&'static str, TestAST)>> for MapVec {
        fn from(o: Vec<(&'static str, TestAST)>) -> Self {
            Self(o)
        }
    }

    #[derive(Debug, Clone)]
    enum TestAST {
        Node {
            id: usize,
            attributes: MapVec,
        },
        #[allow(unused)]
        Terminal(TestToken),
        Literal(super::super::parser::Value),
    }

    impl PartialEq<TestAST> for AST {
        fn eq(&self, other: &TestAST) -> bool {
            other == self
        }
    }

    impl PartialEq<AST> for TestAST {
        fn eq(&self, other: &AST) -> bool {
            match (self, other) {
                (
                    TestAST::Node {
                        id: tid,
                        attributes: tattributes,
                    },
                    AST::Node {
                        nonterminal: id,
                        attributes,
                        ..
                    },
                ) => {
                    NonTerminalId::from(*tid) == *id && {
                        let tattributes = tattributes
                            .0
                            .iter()
                            .map(|(key, value)| (Rc::<str>::from(*key), value))
                            .collect::<HashMap<_, _>>();
                        tattributes.len() == attributes.len()
                            && tattributes.iter().all(|(key, value)| {
                                attributes.get(key).map_or(false, |v| *value == v)
                            })
                    }
                }
                (TestAST::Terminal(ttoken), AST::Terminal(token)) => ttoken == token,
                (TestAST::Literal(tvalue), AST::Literal { value, .. }) => tvalue == value,
                _ => false,
            }
        }
    }

    #[test]
    fn complex_proxy() {
        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<PROXY>"),
            GRAMMAR_PROXY_LEXER,
        ))
        .unwrap();
        EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<PROXY>"), GRAMMAR_PROXY),
            lexer.grammar(),
        )
        .unwrap();
        assert!(EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<PROXY>"), GRAMMAR_PROXY_WRONG_1),
            lexer.grammar()
        )
        .is_err());
        assert!(EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<PROXY>"), GRAMMAR_PROXY_WRONG_2),
            lexer.grammar()
        )
        .is_err());
    }

    #[test]
    fn recognise_handle_empty_rules() {
        let lexer_input = r#""#;
        let grammar_input = r#"
@A ::= <>
 B <>;
B ::= A <>;"#;
        let input = r#""#;
        let lexer =
            Lexer::build_from_plain(StringStream::new(Path::new("<lexer input>"), lexer_input))
                .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<grammar input>"), grammar_input),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        let sets = sets!(
            ==
            A -> . (0)
            A -> . B (0)
            B -> . A (0)
            A -> B . (0)
            B -> A . (0)
        );
        let (recognised, _) = parser
            .recognise(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)))
            .unwrap();
        print_sets(&recognised, &parser, &lexer);
        verify_sets(sets, recognised, &parser, &lexer);
    }

    #[test]
    fn grammar_c() {
        let input = r#"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int main() {
  return sizeof(bool ****);
}
"#;
        let lexer =
            Lexer::build_from_plain(StringStream::new(Path::new("petitc.lx"), GRAMMAR_C_LEXER))
                .unwrap();

        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("petitc.gr"), GRAMMAR_C),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        let _ast = parser
            .parse(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)))
            .unwrap();
    }

    #[test]
    fn grammar_c_prior_assoc() {
        let input = r#"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int main() {
  int a;
  int b;
  a = b = 3+3*2;
  a = a < b > a < b > a;
}
"#;
        let lexer =
            Lexer::build_from_plain(StringStream::new(Path::new("petitc.lx"), GRAMMAR_C_LEXER))
                .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("petitc.gr"), GRAMMAR_C),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        let _ast = parser
            .parse(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)))
            .unwrap();
        // print_ast(&_ast.tree).unwrap();
    }

    #[test]
    fn valid_prefix() {
        let input = r#"1+2+"#;
        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<NUMBERS LEXER>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<NUMBERS>"), GRAMMAR_NUMBERS),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        assert!(parser
            .parse(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)),)
            .is_err());
    }

    #[test]
    fn priority_associativity() {
        // Expected tree:
        // 1+(2+(((3*4)*5)+(6+(7*8))))
        //
        let input = r"1+2+3*4*5+6+7*8";
        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<NUMBERS LEXER>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<NUMBERS IMPROVED>"), GRAMMAR_NUMBERS_IMPROVED),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        let ast = parser
            .parse(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)))
            .unwrap();
        let test_ast = {
            use super::super::parser::Value::*;
            use TestAST::*;
            let add = Literal(Str("AddSub".into()));
            let literal = Literal(Str("Literal".into()));
            let mul = Literal(Str("MulDiv".into()));
            Node {
                id: 0,
                attributes: vec![
                    ("variant", add.clone()),
                    (
                        "left",
                        Node {
                            id: 0,
                            attributes: vec![
                                ("variant", literal.clone()),
                                ("value", Literal(Str("1".into()))),
                            ]
                            .into(),
                        },
                    ),
                    (
                        "right",
                        Node {
                            id: 0,
                            attributes: vec![
                                ("variant", add.clone()),
                                (
                                    "left",
                                    Node {
                                        id: 0,
                                        attributes: vec![
                                            ("variant", literal.clone()),
                                            ("value", Literal(Str("2".into()))),
                                        ]
                                        .into(),
                                    },
                                ),
                                (
                                    "right",
                                    Node {
                                        id: 0,
                                        attributes: vec![
                                            ("variant", add.clone()),
                                            (
                                                "left",
                                                Node {
                                                    id: 0,
                                                    attributes: vec![
                                                        ("variant", mul.clone()),
                                                        (
                                                            "left",
                                                            Node {
                                                                id: 0,
                                                                attributes: vec![
                                                                    ("variant", mul.clone()),
                                                                    (
                                                                        "left",
                                                                        Node {
                                                                            id: 0,
                                                                            attributes: vec![
									    ("variant", literal.clone()),
									    ("value", Literal(Str("3".into()))),
									]
                                                                            .into(),
                                                                        },
                                                                    ),
                                                                    (
                                                                        "right",
                                                                        Node {
                                                                            id: 0,
                                                                            attributes: vec![
									    ("variant", literal.clone()),
									    ("value", Literal(Str("4".into()))),
									]
                                                                            .into(),
                                                                        },
                                                                    ),
                                                                ]
                                                                .into(),
                                                            },
                                                        ),
                                                        (
                                                            "right",
                                                            Node {
                                                                id: 0,
                                                                attributes: vec![
                                                                    (
                                                                        "variant",
                                                                        literal.clone(),
                                                                    ),
                                                                    (
                                                                        "value",
                                                                        Literal(
                                                                            Str("5".into()),
                                                                        ),
                                                                    ),
                                                                ]
                                                                .into(),
                                                            },
                                                        ),
                                                    ]
                                                    .into(),
                                                },
                                            ),
                                            (
                                                "right",
                                                Node {
                                                    id: 0,
                                                    attributes: vec![
                                                        ("variant", add),
                                                        (
                                                            "left",
                                                            Node {
                                                                id: 0,
                                                                attributes: vec![
                                                                    (
                                                                        "variant",
                                                                        literal.clone(),
                                                                    ),
                                                                    (
                                                                        "value",
                                                                        Literal(
                                                                            Str("6".into()),
                                                                        ),
                                                                    ),
                                                                ]
                                                                .into(),
                                                            },
                                                        ),
                                                        (
                                                            "right",
                                                            Node {
                                                                id: 0,
                                                                attributes: vec![
                                                                    ("variant", mul),
                                                                    (
                                                                        "left",
                                                                        Node {
                                                                            id: 0,
                                                                            attributes: vec![
									("variant", literal.clone()),
									("value", Literal(Str("7".into()))),
								    ]
                                                                            .into(),
                                                                        },
                                                                    ),
                                                                    (
                                                                        "right",
                                                                        Node {
                                                                            id: 0,
                                                                            attributes: vec![
									("variant", literal),
									("value", Literal(Str("8".into()))),
								    ]
                                                                            .into(),
                                                                        },
                                                                    ),
                                                                ]
                                                                .into(),
                                                            },
                                                        ),
                                                    ]
                                                    .into(),
                                                },
                                            ),
                                        ]
                                        .into(),
                                    },
                                ),
                            ]
                            .into(),
                        },
                    ),
                ]
                .into(),
            }
        };
        assert_eq!(ast.tree, test_ast,);
    }

    #[test]
    fn ast_builder() {
        let input = r#"1+(2*3-4)"#;

        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<lexer input>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<grammar input>"), GRAMMAR_NUMBERS),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        let mut input_stream = StringStream::new(Path::new("<input>"), input);
        let mut lexed_input = lexer.lex(&mut input_stream);
        let (table, raw_input) = parser.recognise(&mut lexed_input).unwrap();
        let forest = parser.to_forest(&table, &raw_input).unwrap();
        let ast = parser.select_ast(&forest, &raw_input, lexed_input.last_span());

        let test_ast = {
            use super::super::parser::Value::*;
            use TestAST::*;
            Node {
                id: 0,
                attributes: vec![
                    (
                        "right",
                        Node {
                            id: 1,
                            attributes: vec![(
                                "self",
                                Node {
                                    id: 0,
                                    attributes: vec![
                                        (
                                            "right",
                                            Node {
                                                id: 1,
                                                attributes: vec![(
                                                    "self",
                                                    Literal(Str("4".into())),
                                                )]
                                                .into(),
                                            },
                                        ),
                                        (
                                            "left",
                                            Node {
                                                id: 0,
                                                attributes: vec![(
                                                    "self",
                                                    Node {
                                                        id: 1,
                                                        attributes: vec![
                                                            (
                                                                "right",
                                                                Node {
                                                                    id: 2,
                                                                    attributes: vec![(
                                                                        "self",
                                                                        Literal(
                                                                            Str("3".into()),
                                                                        ),
                                                                    )]
                                                                    .into(),
                                                                },
                                                            ),
                                                            (
                                                                "left",
                                                                Node {
                                                                    id: 1,
                                                                    attributes: vec![(
                                                                        "self",
                                                                        Literal(
                                                                            Str("2".into()),
                                                                        ),
                                                                    )]
                                                                    .into(),
                                                                },
                                                            ),
                                                        ]
                                                        .into(),
                                                    },
                                                )]
                                                .into(),
                                            },
                                        ),
                                    ]
                                    .into(),
                                },
                            )]
                            .into(),
                        },
                    ),
                    (
                        "left",
                        Node {
                            id: 0,
                            attributes: vec![(
                                "self",
                                Node {
                                    id: 1,
                                    attributes: vec![("self", Literal(Str("1".into())))].into(),
                                },
                            )]
                            .into(),
                        },
                    ),
                ]
                .into(),
            }
        };

        assert_eq!(ast, test_ast, "Expected\n{:#?}\n\nGot\n{:?}", test_ast, ast);
    }

    #[test]
    fn forest_builder() {
        let input = r#"1+(2*3-4)"#;

        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<lexer input>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<grammar input>"), GRAMMAR_NUMBERS),
            lexer.grammar(),
        )
        .unwrap();

        let parser = EarleyParser::new(grammar);
        let sets = final_sets!(
            (parser.grammar())
        (lexer.grammar())
            ==
            Factor -> NUMBER (1)
            Product -> Factor (1)
            Sum -> Product (1)
            Sum -> Sum PM Product (9)

            ==

            ==
            Factor -> LPAR Sum RPAR (9)
            Product -> Factor (9)

            ==
            Factor -> NUMBER (4)
            Product -> Factor (4)
            Sum -> Product (4)
            Product -> Product TD Factor (6)
            Sum -> Product (6)
            Sum -> Sum PM Product (8)

            ==

            ==
            Factor -> NUMBER (6)

            ==

        ==
        Factor -> NUMBER (8)
            Product -> Factor (8)

            ==

            ==
            );

        let (table, raw_input) = parser
            .recognise(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)))
            .unwrap();
        let forest = parser.to_forest(&table, &raw_input).unwrap();
        assert_eq!(
            forest,
            sets,
            "Parsed forest:\n{}\nExpected forest:\n{}",
            forest
                .iter()
                .map(|set| format!("{}", set))
                .collect::<Vec<_>>()
                .join("\n"),
            sets.iter()
                .map(|set| format!("{}", set))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn recogniser() {
        let input = r#"1+(2*3-4)"#;

        let lexer = Lexer::build_from_plain(StringStream::new(
            Path::new("<lexer input>"),
            GRAMMAR_NUMBERS_LEXER,
        ))
        .unwrap();
        let grammar = EarleyGrammar::build_from_plain(
            StringStream::new(Path::new("<grammar input>"), GRAMMAR_NUMBERS),
            lexer.grammar(),
        )
        .unwrap();
        let parser = EarleyParser::new(grammar);
        let sets = sets!(
            ==
            Sum -> . Sum PM Product (0)
            Sum -> . Product (0)
            Product -> . Product TD Factor (0)
            Product -> . Factor (0)
            Factor -> . LPAR Sum RPAR (0)
            Factor -> . NUMBER (0)

            ==
            Factor -> NUMBER . (0)
            Product -> Factor . (0)
            Sum -> Product . (0)
            Product -> Product . TD Factor (0)
            Sum -> Sum . PM Product (0)

            ==
            Sum -> Sum PM . Product (0)
            Product -> . Product TD Factor (2)
            Product -> . Factor (2)
            Factor -> . LPAR Sum RPAR (2)
            Factor -> . NUMBER (2)

            ==
            Factor -> LPAR . Sum RPAR (2)
            Sum -> . Sum PM Product (3)
            Sum -> . Product (3)
            Product -> . Product TD Factor (3)
            Product -> . Factor (3)
            Factor -> . LPAR Sum RPAR (3)
            Factor -> . NUMBER (3)

            ==
            Factor -> NUMBER . (3)
            Product -> Factor . (3)
            Sum -> Product . (3)
            Product -> Product . TD Factor (3)
            Factor -> LPAR Sum . RPAR (2)
            Sum -> Sum . PM Product (3)

            ==
            Product -> Product TD . Factor (3)
            Factor -> . LPAR Sum RPAR (5)
            Factor -> . NUMBER (5)

            ==
            Factor -> NUMBER . (5)
            Product -> Product TD Factor . (3)
            Sum -> Product . (3)
            Product -> Product . TD Factor (3)
            Factor -> LPAR Sum . RPAR (2)
            Sum -> Sum . PM Product (3)

            ==
            Sum -> Sum PM . Product (3)
            Product -> . Product TD Factor (7)
            Product -> . Factor (7)
            Factor -> . LPAR Sum RPAR (7)
            Factor -> . NUMBER (7)

            ==
            Factor -> NUMBER . (7)
            Product -> Factor . (7)
            Sum -> Sum PM Product . (3)
            Product -> Product . TD Factor (7)
            Factor -> LPAR Sum . RPAR (2)
            Sum -> Sum . PM Product (3)

            ==
            Factor -> LPAR Sum RPAR . (2)
            Product -> Factor . (2)
            Sum -> Sum PM Product . (0)
            Product -> Product . TD Factor (2)
            Sum -> Sum . PM Product (0)
        );
        let (recognised, _) = parser
            .recognise(&mut lexer.lex(&mut StringStream::new(Path::new("<input>"), input)))
            .unwrap();
        verify_sets(sets, recognised, &parser, &lexer);
    }

    fn verify_sets(
        sets: Vec<Vec<TestEarleyItem>>,
        recognised: Vec<StateSet>,
        parser: &EarleyParser,
        lexer: &Lexer,
    ) {
        assert_eq!(recognised.len(), sets.len());
        for (set, (expected, recognised)) in sets.iter().zip(recognised.iter()).enumerate() {
            assert_eq!(
                expected.len(),
                recognised.set.len(),
                "Set #{} length does not match.",
                set
            );
            for (item_nb, (test_item, item)) in
                expected.iter().zip(recognised.set.iter()).enumerate()
            {
                test_item.matches(item, parser, lexer, set, item_nb);
            }
        }
    }
}
